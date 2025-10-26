<script lang="ts">
	import { WASI } from '@bjorn3/browser_wasi_shim';

	let status = $state<string>('Not loaded');
	let error = $state<string | null>(null);

	// Use plain array, then copy to reactive state only once
	let logsArray: string[] = [];
	let logs = $state<string[]>([]);

	// Global flag to ensure this only runs once, even if module reloads
	const GLOBAL_KEY = '__QUANTY_WASM_LOADED__';

	// Run once immediately when module loads (client-side only due to ssr: false)
	if (typeof window !== 'undefined' && !(window as any)[GLOBAL_KEY]) {
		(window as any)[GLOBAL_KEY] = true;
		console.log('WASM test: starting load (setting global flag)');
		(async () => {
			try {
				status = 'Loading WASM module...';
				console.log('WASM test: Loading FFI glue...');

				// Import the FFI glue code
				const ghc_wasm_jsffi_module = await import('$lib/wasm/ghc_wasm_jsffi.js');
				console.log('WASM test: FFI glue loaded', ghc_wasm_jsffi_module);
				const ghc_wasm_jsffi = ghc_wasm_jsffi_module.default;

				// Track calls to see if handlers are being called repeatedly
				let stdoutCallCount = 0;
				let stderrCallCount = 0;

				// Create WASI instance with stdout/stderr capture
				// Use NON-REACTIVE array to eliminate any Svelte magic
				const stdout_handler = (str: string) => {
					stdoutCallCount++;
					console.log(`WASM stdout (call #${stdoutCallCount}):`, str);
					logsArray.push(`[stdout call #${stdoutCallCount}] ${str}`);
				};

				const stderr_handler = (str: string) => {
					stderrCallCount++;
					console.error(`WASM stderr (call #${stderrCallCount}):`, str);
					logsArray.push(`[stderr call #${stderrCallCount}] ${str}`);
				};

			const wasi = new WASI([], [], [
				{
					path: '/dev/stdin',
					fd_filestat_get: () => ({
						type: 0,
						linkcount: 0n,
						size: 0n,
						atim: 0n,
						mtim: 0n,
						ctim: 0n
					})
				} as any,
				{
					path: '/dev/stdout',
					fd_write: (view: Uint8Array) => {
						const text = new TextDecoder().decode(view);
						stdout_handler(text);
						return view.byteLength;
					}
				} as any,
				{
					path: '/dev/stderr',
					fd_write: (view: Uint8Array) => {
						const text = new TextDecoder().decode(view);
						stderr_handler(text);
						return view.byteLength;
					}
				} as any
			]);

				status = 'Instantiating WASM...';

				// Create exports object for FFI
				const instance_exports: any = {};

				// Fetch and instantiate WASM module
				const { instance } = await WebAssembly.instantiateStreaming(
					fetch('/wasm/dist/quanty.wasm'),
					{
						wasi_snapshot_preview1: wasi.wasiImport,
						ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports)
					}
				);

				// Populate exports
				Object.assign(instance_exports, instance.exports);

				status = 'Initializing Haskell runtime...';

				// Initialize WASI
				wasi.initialize(instance);

				// Initialize Haskell runtime
				await instance_exports.hs_init();

				status = 'Testing pure function addNumbers()...';

				// Test pure function first (no IO, should have no scheduler issues)
				// Note: GHC WASM wraps all exports in Promises
				console.log('WASM test: About to call addNumbers(5, 3)');
				const result = await instance_exports.addNumbers(5, 3);
				console.log('WASM test: addNumbers(5, 3) =', result);
				logsArray.push(`addNumbers(5, 3) = ${result}`);

				status = 'Testing IO function helloWasm()...';

				// Now test helloWasm() which uses console.log instead of putStrLn
				console.log('WASM test: About to call helloWasm()');
				await instance_exports.helloWasm();
				console.log('WASM test: Returned from helloWasm()');

				status = 'Testing bidirectional data passing with doubleValue()...';

				// Test doubleValue - demonstrates JS -> Haskell -> JS data flow
				console.log('WASM test: About to call doubleValue(21)');
				const doubled = await instance_exports.doubleValue(21);
				console.log('WASM test: doubleValue(21) =', doubled);
				logsArray.push(`doubleValue(21) = ${doubled}`);

				if (doubled !== 42) {
					console.error('ERROR: Expected 42, got', doubled);
					throw new Error(`doubleValue test failed: expected 42, got ${doubled}`);
				}

				// Wait a moment just in case
				await new Promise(resolve => setTimeout(resolve, 50));

				// Copy to reactive state ONCE at the end
				logs = [...logsArray];

				status = '✅ Success! Check the logs above and browser console.';
				console.log('WASM test: All done! stdout was called', stdoutCallCount, 'times');
			} catch (err) {
				console.error('WASM load error:', err);
				error = err instanceof Error ? `${err.message}\n\nStack: ${err.stack}` : String(err);
				status = '❌ Failed to load WASM module';
			}
		})();
	}
</script>

<div class="container mx-auto max-w-4xl p-8">
	<h1 class="text-4xl font-bold mb-4">WASM Test Page</h1>

	<div class="mb-8">
		<h2 class="text-2xl font-semibold mb-2">Status</h2>
		<p class="text-lg font-mono p-4 bg-slate-100 dark:bg-slate-800 rounded">{status}</p>
	</div>

	{#if error}
		<div class="mb-8">
			<h2 class="text-2xl font-semibold mb-2 text-red-600">Error</h2>
			<pre class="p-4 bg-red-50 dark:bg-red-900/20 rounded overflow-auto">{error}</pre>
		</div>
	{/if}

	<div class="mb-8">
		<h2 class="text-2xl font-semibold mb-2">WASM Output Logs</h2>
		{#if logs.length === 0}
			<p class="text-slate-500 italic">No output yet...</p>
		{:else}
			<div class="bg-slate-900 text-green-400 font-mono p-4 rounded overflow-auto">
				{#each logs as log}
					<div>{log}</div>
				{/each}
			</div>
		{/if}
	</div>

	<div class="bg-blue-50 dark:bg-blue-900/20 p-4 rounded">
		<h3 class="font-semibold mb-2">What's happening?</h3>
		<ol class="list-decimal list-inside space-y-1 text-sm">
			<li>Loading Haskell WASM module (quanty.wasm) and FFI glue</li>
			<li>Initializing WASI (WebAssembly System Interface) shim</li>
			<li>Initializing Haskell runtime (hs_init)</li>
			<li>Calling the <code class="bg-white/50 px-1 rounded">helloWasm()</code> function from Haskell</li>
			<li>
				Expected output: <code class="bg-white/50 px-1 rounded">"Hello from Haskell WASM!"</code> in the logs
			</li>
		</ol>
	</div>

	<div class="mt-8">
		<a href="/" class="text-blue-600 hover:underline">← Back to home</a>
	</div>
</div>
