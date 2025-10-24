import { Effect } from "effect"
import { createQuery, createMutation } from "@tanstack/svelte-query"

/**
 * Creates a TanStack Query that runs an Effect.
 *
 * @param cacheKey - Unique identifier for this query (used for caching and refetching)
 * @param effect - The Effect to run when the query executes
 *
 * @example
 * ```ts
 * const userQuery = createQueryFx(["user", userId], getUser)
 * ```
 */
export const createQueryFx = <
  TData,
  TError,
  TQueryKey extends readonly unknown[],
>(
  cacheKey: TQueryKey,
  effect: Effect.Effect<TData, TError, never>,
) =>
  createQuery(() => ({
    queryKey: cacheKey,
    queryFn: (): Promise<TData> => Effect.runPromise(effect),
  }))

/**
 * Creates a TanStack Mutation that runs an Effect.
 *
 * @param effect - The Effect to run when the mutation executes
 *
 * @example
 * ```ts
 * const createUserMutation = createMutationFx(createUser)
 * createUserMutation.mutate()
 * ```
 */
export const createMutationFx = <TData, TError>(
  effect: Effect.Effect<TData, TError, never>,
) =>
  createMutation(() => ({
    mutationFn: (): Promise<TData> => Effect.runPromise(effect),
  }))
