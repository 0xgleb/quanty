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
 */
export const createQuery = <TData, TError>(
  effect: Effect.Effect<TData, TError, never>,
): QueryResult<TData, TError> => {
  let state = $state<QueryState<TData, TError>>({ status: "pending" })

  const fetch = () => {
    state = { status: "pending" }
    Effect.runPromise(effect)
      .then(data => (state = { status: "success", data }))
      .catch(error => (state = { status: "error", error }))
  }

  // Auto-fetch on mount
  $effect(() => {
    untrack(fetch)
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
 */
export const createMutation = <TData, TError>(
  effect: Effect.Effect<TData, TError, never>,
): MutationResult<TData, TError> => {
  let state = $state<MutationState<TData, TError>>({ status: "idle" })

  const mutate = () => {
    state = { status: "pending" }
    Effect.runPromise(effect)
      .then(data => (state = { status: "success", data }))
      .catch(error => (state = { status: "error", error }))
  }

  const reset = () => {
    state = { status: "idle" }
  }

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
