import { Effect, Fiber, Exit, Cause } from "effect"
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
 * Automatically interrupts in-flight Effects when component unmounts.
 */
export const createQuery = <TData, TError>(
  effect: Effect.Effect<TData, TError, never>,
): QueryResult<TData, TError> => {
  let state = $state<QueryState<TData, TError>>({ status: "pending" })
  let currentFiber: Fiber.RuntimeFiber<TData, TError> | null = null

  const fetch = () => {
    // Interrupt previous fiber if still running
    if (currentFiber) {
      Effect.runFork(Fiber.interrupt(currentFiber))
    }

    state = { status: "pending" }
    currentFiber = Effect.runFork(effect)

    Effect.runPromise(Fiber.await(currentFiber)).then(exit => {
      if (Exit.isSuccess(exit)) {
        state = { status: "success", data: exit.value }
      } else if (Exit.isFailure(exit) && !Cause.isInterrupted(exit.cause)) {
        const error = Cause.failureOption(exit.cause)
        if (error._tag === "Some") {
          state = { status: "error", error: error.value }
        }
      }
    })
  }

  // Auto-fetch on mount, cancel on unmount
  $effect(() => {
    untrack(fetch)

    return () => {
      // Cleanup: interrupt in-flight fiber when component unmounts
      if (currentFiber) {
        Effect.runFork(Fiber.interrupt(currentFiber))
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

type MutationResult<TInput, TData, TError> = {
  data: TData | undefined
  error: TError | undefined
  isPending: boolean
  isSuccess: boolean
  isError: boolean
  isIdle: boolean
  mutate: TInput extends void ? () => void : (input: TInput) => void
  reset: () => void
}

/**
 * Create a mutation for manual triggering (e.g., button clicks).
 * Similar to TanStack Query mutations but built for Svelte 5 runes + Effect.
 *
 * Automatically interrupts in-flight Effects when component unmounts.
 */
export const createMutation = <TInput, TData, TError>(
  effectFn: (input: TInput) => Effect.Effect<TData, TError, never>,
): MutationResult<TInput, TData, TError> => {
  let state = $state<MutationState<TData, TError>>({ status: "idle" })
  let currentFiber: Fiber.RuntimeFiber<TData, TError> | null = null

  const mutate = ((input?: TInput) => {
    // Interrupt previous fiber if still running
    if (currentFiber) {
      Effect.runFork(Fiber.interrupt(currentFiber))
    }

    state = { status: "pending" }
    currentFiber = Effect.runFork(effectFn(input as TInput))

    Effect.runPromise(Fiber.await(currentFiber)).then(exit => {
      if (Exit.isSuccess(exit)) {
        state = { status: "success", data: exit.value }
      } else if (Exit.isFailure(exit) && !Cause.isInterrupted(exit.cause)) {
        const error = Cause.failureOption(exit.cause)
        if (error._tag === "Some") {
          state = { status: "error", error: error.value }
        }
      }
    })
  }) as TInput extends void ? () => void : (input: TInput) => void

  const reset = () => {
    state = { status: "idle" }
  }

  // Interrupt in-flight fiber when component unmounts
  $effect(() => {
    return () => {
      if (currentFiber) {
        Effect.runFork(Fiber.interrupt(currentFiber))
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
