import { Effect } from "effect"
import { untrack } from "svelte"

type QueryState<TData, TError> =
  | { status: "pending" }
  | { status: "success"; data: TData }
  | { status: "error"; error: TError }

type QueryResult<TData, TError> = {
  data: TData | undefined
  error: TError | undefined
  isPending: boolean
  isSuccess: boolean
  isError: boolean
  refetch: () => void
}

/**
 * Create a query that automatically fetches on mount and provides reactive state.
 * Similar to TanStack Query but built for Svelte 5 runes + Effect.
 *
 * Automatically cancels in-flight requests when component unmounts.
 */
export const createQuery = <TData, TError>(
  effect: Effect.Effect<TData, TError, never>,
): QueryResult<TData, TError> => {
  let state = $state<QueryState<TData, TError>>({ status: "pending" })
  let abortController: AbortController | null = null

  const fetch = () => {
    // Cancel previous request if still running
    if (abortController) {
      abortController.abort()
    }

    abortController = new AbortController()
    const signal = abortController.signal

    state = { status: "pending" }
    Effect.runPromise(effect)
      .then(data => {
        if (!signal.aborted) {
          state = { status: "success", data }
        }
      })
      .catch(error => {
        if (!signal.aborted) {
          state = { status: "error", error }
        }
      })
  }

  // Auto-fetch on mount, cancel on unmount
  $effect(() => {
    untrack(fetch)

    return () => {
      // Cleanup: cancel in-flight request when component unmounts
      if (abortController) {
        abortController.abort()
      }
    }
  })

  return {
    get data() {
      return state.status === "success" ? state.data : undefined
    },
    get error() {
      return state.status === "error" ? state.error : undefined
    },
    get isPending() {
      return state.status === "pending"
    },
    get isSuccess() {
      return state.status === "success"
    },
    get isError() {
      return state.status === "error"
    },
    refetch: fetch,
  }
}

type MutationState<TData, TError> =
  | { status: "idle" }
  | { status: "pending" }
  | { status: "success"; data: TData }
  | { status: "error"; error: TError }

type MutationResult<TData, TError> = {
  data: TData | undefined
  error: TError | undefined
  isPending: boolean
  isSuccess: boolean
  isError: boolean
  isIdle: boolean
  mutate: () => void
  reset: () => void
}

/**
 * Create a mutation for manual triggering (e.g., button clicks).
 * Similar to TanStack Query mutations but built for Svelte 5 runes + Effect.
 *
 * Automatically cancels in-flight requests when component unmounts.
 */
export const createMutation = <TData, TError>(
  effect: Effect.Effect<TData, TError, never>,
): MutationResult<TData, TError> => {
  let state = $state<MutationState<TData, TError>>({ status: "idle" })
  let abortController: AbortController | null = null

  const mutate = () => {
    // Cancel previous request if still running
    if (abortController) {
      abortController.abort()
    }

    abortController = new AbortController()
    const signal = abortController.signal

    state = { status: "pending" }
    Effect.runPromise(effect)
      .then(data => {
        if (!signal.aborted) {
          state = { status: "success", data }
        }
      })
      .catch(error => {
        if (!signal.aborted) {
          state = { status: "error", error }
        }
      })
  }

  const reset = () => {
    state = { status: "idle" }
  }

  // Cancel in-flight request when component unmounts
  $effect(() => {
    return () => {
      if (abortController) {
        abortController.abort()
      }
    }
  })

  return {
    get data() {
      return state.status === "success" ? state.data : undefined
    },
    get error() {
      return state.status === "error" ? state.error : undefined
    },
    get isPending() {
      return state.status === "pending"
    },
    get isSuccess() {
      return state.status === "success"
    },
    get isError() {
      return state.status === "error"
    },
    get isIdle() {
      return state.status === "idle"
    },
    mutate,
    reset,
  }
}
