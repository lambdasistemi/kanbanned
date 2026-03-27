# Kanbanned Constitution

## Core Principles

### I. Terminal-Native

Kanbanned is a Haskell TUI application. All rendering goes through libvterm-haskell and ANSI escape sequences to the real terminal. No browser, no web server, no external GUI dependencies.

### II. Shared Kanban State

GitHub Projects v2 is the single source of truth for kanban board state (Backlog/WIP/Done). The same board is accessible from gh-dashboard (browser) and kanbanned (terminal) interchangeably. No local-only state for board items.

### III. Agent-Daemon Integration

Agent sessions are managed through agent-daemon's REST+WebSocket API. Terminal I/O from agent sessions is rendered via libvterm. Session lifecycle (launch, attach, detach, stop) maps to kanban column transitions.

### IV. Correctness First

Haskell type system enforced at boundaries. JSON decoding must handle all GitHub API shapes. WebSocket reconnection must be robust. No partial functions, no unsafe operations.

### V. Nix-First Build

All builds, tests, and CI go through Nix. Development shell provides all tools. CI uses the build gate pattern to pre-cache derivations.

### VI. Hackage-Ready Quality

The project must pass `cabal check`, fourmolu formatting (70-char, leading commas), hlint, and `-Wall -Werror` at all times. Haddock on all exports.

## Architecture Constraints

- **libvterm-haskell** is the rendering engine (not brick, not vty)
- **GitHub GraphQL API** for Projects v2 queries and mutations
- **agent-daemon REST API** for session management
- **agent-daemon WebSocket** for terminal I/O streams
- GHC 9.8.4 via haskell.nix

## Development Workflow

- Linear git history (rebase, no merge commits)
- Conventional commits for release-please
- PRs require Build Gate CI check to pass
- Feature branches from main, worktree-based development
- Documentation updated before code

## Governance

Constitution supersedes all other practices. Amendments require documentation and approval.

**Version**: 1.0.0 | **Ratified**: 2026-03-27
