# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

Like all other AI agents working in this repo, Claude Code MUST obey @AGENTS.md

## Effect TypeScript

This project uses the Effect TypeScript library for frontend development. All
frontend code should follow Effect patterns as defined in AGENTS.md, including:

- Effect for async operations and error handling
- Effect Schema for runtime validation
- Effect Services and Layers for dependency injection
- Explicit error modeling with tagged unions
- Effect Streams for real-time data

See AGENTS.md for detailed Effect coding patterns and standards.
