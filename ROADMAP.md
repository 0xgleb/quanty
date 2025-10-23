# Quanty Development Roadmap

## Overview

This roadmap outlines the phased development of Quanty, a web application for
pricing cryptocurrency options and financial derivatives. Development follows a
vertical slice approach - each phase delivers complete, usable features from
backend API through to frontend UI.

**Development Approach**: Feature-by-feature implementation - each phase adds
complete vertical slices (types, models, API, UI, tests) to avoid dead code and
enable incremental delivery.

---

## Phase 0: Project Foundation

Foundation infrastructure, development environment, and initial project
structure.

### Deliverables

- [x] Nix flake with development environment
- [x] Haskell project structure (Stack + package.yaml)
- [x] Pre-commit hooks (fourmolu, hlint, prettier, eslint)
- [x] Initial documentation (README, SPEC, ROADMAP, CLAUDE)
- [ ] SvelteKit frontend setup
- [ ] Basic CI/CD pipeline

### Tasks

- [x] Setup Nix flake with GHC, Stack, Node.js, pnpm
- [x] Configure Stack to use Nix system-ghc
- [x] Setup pre-commit hooks for code quality
- [x] Create project documentation
- [ ] [#2](https://github.com/0xgleb/quanty/issues/2) - Initialize frontend with
      shadcn-svelte
- [ ] [#3](https://github.com/0xgleb/quanty/issues/3) - Setup GitHub Actions CI
      pipeline

---

## Phase 1: Backend MVP - Black-Scholes API

Minimal viable backend with Black-Scholes pricing endpoint.

### Deliverables

- REST API server with health check
- Black-Scholes pricing endpoint (complete feature)
- JSON serialization with Aeson
- Comprehensive test suite

### Tasks

- [ ] [#6](https://github.com/0xgleb/quanty/issues/6) - Implement Black-Scholes
      pricing in `src/BlackScholes/`
- [ ] [#7](https://github.com/0xgleb/quanty/issues/7) - Create basic Servant
      server

---

## Phase 2: Frontend MVP - Basic Calculator

Minimal viable frontend with option pricing calculator.

### Deliverables

- Home page with project description
- Black-Scholes calculator page
- Parameter input form with validation
- Results display with price and Greeks
- Responsive design
- End-to-end tests

### Tasks

- [ ] [#8](https://github.com/0xgleb/quanty/issues/8) - Build basic calculator
      UI

---

## Phase 3: Visualization - Charts and Graphs

Add visual components for better understanding of option pricing.

### Deliverables

- Payoff diagram showing profit/loss at expiration
- Greeks visualization charts
- Interactive parameter sliders
- Real-time chart updates as parameters change

### Tasks

**Charting Library**:

- [ ] Setup TradingView Lightweight Charts with TypeScript types
- [ ] Create reusable chart wrapper components
- [ ] Configure chart theming and styling

**Payoff Diagrams** (`frontend/src/lib/components/charts/`):

- [ ] Create `PayoffDiagram.svelte` component
- [ ] Calculate payoff at various spot prices
- [ ] Plot option value vs spot price
- [ ] Add breakeven point markers
- [ ] Show max profit/loss

**Greeks Charts**:

- [ ] Create `GreeksChart.svelte` component
- [ ] Plot Delta curve vs spot price
- [ ] Plot Gamma curve vs spot price
- [ ] Plot Vega curve vs volatility
- [ ] Plot Theta curve vs time to expiry

**Interactive Controls**:

- [ ] Add slider inputs for parameters
- [ ] Update charts in real-time as sliders move
- [ ] Debounce API calls to avoid excessive requests
- [ ] Add preset scenarios (ATM, ITM, OTM)

**UI/UX Enhancements**:

- [ ] Add dark mode support
- [ ] Improve responsive design for mobile
- [ ] Add tooltips explaining Greeks
- [ ] Add help text and documentation links

---

## Phase 4: Market Data Integration

Integrate real-time and historical cryptocurrency market data.

### Deliverables

- Real-time price feeds from crypto exchanges
- Historical price data and volatility calculation
- Market data caching layer
- Frontend integration with symbol selection

### Tasks

**Market Data Feature** (`src/MarketData/`):

Complete market data feature in a single module containing:

- [ ] Define types: `Price`, `OHLCV`, `Symbol`, `TimeRange`, `VolatilityMethod`
- [ ] Create exchange API client trait/interface
- [ ] Implement Binance API client
- [ ] Implement Coinbase API client (optional)
- [ ] Add HTTP client with connection pooling and rate limiting
- [ ] Implement historical volatility calculation (close-to-close, Parkinson,
      Garman-Klass)
- [ ] Add in-memory cache with TTL and invalidation logic
- [ ] Define API endpoints: `GET /api/v1/market/price/:symbol`,
      `/history/:symbol`, `/volatility/:symbol`
- [ ] Implement API handlers with error handling
- [ ] Add fallback to cached data on API failures
- [ ] Add retry logic with exponential backoff
- [ ] Unit tests for volatility calculations
- [ ] Integration tests for exchange clients

**Frontend Integration**:

- [ ] Add symbol selector dropdown component
- [ ] Auto-populate spot price from market data
- [ ] Auto-populate volatility from historical data
- [ ] Show last updated timestamp
- [ ] Add manual override option
- [ ] Show data staleness warnings

---

## Phase 5: Advanced Pricing Models

Expand pricing capabilities with additional models.

### Deliverables

- Binomial tree pricing for American options
- Monte Carlo simulation
- Implied volatility calculation
- Model comparison view

### Tasks

**Binomial Tree Feature** (`src/Binomial/`):

Complete binomial tree pricing feature:

- [ ] Define types: `BinomialInput`, `TreeConfig`, `BinomialResult`
- [ ] Implement Cox-Ross-Rubinstein binomial tree algorithm
- [ ] Support American exercise style
- [ ] Add configurable number of time steps
- [ ] Implement Greeks via finite differences
- [ ] Define API endpoint: `POST /api/v1/price/binomial`
- [ ] Implement API handler
- [ ] Add memoization for tree nodes
- [ ] Unit tests for binomial pricing
- [ ] Property tests comparing to Black-Scholes for European options

**Monte Carlo Feature** (`src/MonteCarlo/`):

Complete Monte Carlo simulation feature:

- [ ] Define types: `MonteCarloInput`, `SimulationConfig`, `MonteCarloResult`
- [ ] Implement geometric Brownian motion simulation
- [ ] Add configurable paths and time steps
- [ ] Add variance reduction (antithetic variates)
- [ ] Define API endpoint: `POST /api/v1/price/monte-carlo`
- [ ] Implement API handler
- [ ] Add parallel computation support
- [ ] Add progress updates for long computations
- [ ] Unit tests for Monte Carlo
- [ ] Convergence tests

**Implied Volatility Feature** (`src/ImpliedVol/`):

Complete implied volatility calculation feature:

- [ ] Define types: `IVInput`, `IVResult`, `SolverMethod`
- [ ] Implement Newton-Raphson solver
- [ ] Add Brent's method as fallback
- [ ] Define API endpoint: `POST /api/v1/iv/calculate`
- [ ] Implement API handler
- [ ] Unit tests for IV calculation
- [ ] Test with known market data

**Frontend Integration**:

- [ ] Add model selector dropdown (Black-Scholes, Binomial, Monte Carlo)
- [ ] Show model-specific parameters UI
- [ ] Add model comparison view (side-by-side results)
- [ ] Display computation time for each model

---

## Phase 6: Portfolio and Strategy Analysis

Tools for analyzing option portfolios and multi-leg strategies.

### Deliverables

- Multi-option position builder
- Portfolio Greeks aggregation
- Strategy templates
- Risk metrics and payoff diagrams

### Tasks

**Portfolio Feature** (`src/Portfolio/`):

Complete portfolio analysis feature:

- [ ] Define types: `Position`, `Portfolio`, `Strategy`, `RiskMetrics`
- [ ] Implement portfolio value calculation
- [ ] Implement portfolio Greeks aggregation
- [ ] Implement position-level Greeks
- [ ] Implement Value at Risk (VaR) calculation
- [ ] Calculate maximum loss scenarios
- [ ] Calculate breakeven points
- [ ] Add profit probability estimates
- [ ] Define strategy templates: long call/put, covered call, protective put,
      straddle, strangle, spreads, iron condor
- [ ] Define API endpoints: `POST /api/v1/portfolio/analyze`,
      `GET /api/v1/strategies`, `POST /api/v1/strategy/build`
- [ ] Implement API handlers
- [ ] Unit tests for portfolio calculations
- [ ] Unit tests for risk metrics

**Frontend Integration**:

- [ ] Add position builder UI component
- [ ] Show multi-leg payoff diagrams
- [ ] Display portfolio Greeks table
- [ ] Add strategy template selector
- [ ] Show risk metrics dashboard
- [ ] Add save/load portfolio functionality

---

## Phase 7: Real-Time Features

Add live data streaming and collaborative features.

### Deliverables

- WebSocket streaming for live market data
- Live option prices and Greeks updates
- Real-time chart updates
- Mobile-responsive design improvements

### Tasks

**WebSocket Backend**:

- [ ] Add WebSocket endpoint for market data stream
- [ ] Implement subscription management
- [ ] Add heartbeat/keepalive
- [ ] Handle reconnection logic

**Live Price Updates**:

- [ ] Stream live prices from exchanges
- [ ] Calculate Greeks in real-time
- [ ] Push updates to connected clients
- [ ] Add update throttling (max 1 update/sec)

**Frontend WebSocket Client**:

- [ ] Create WebSocket connection manager
- [ ] Handle connection lifecycle (connect, disconnect, reconnect)
- [ ] Update UI reactively on price changes
- [ ] Show connection status indicator

**Real-Time Charts**:

- [ ] Update payoff diagrams in real-time
- [ ] Update Greeks charts in real-time
- [ ] Add animation for smooth transitions
- [ ] Add time series price chart

**Mobile Optimization**:

- [ ] Optimize layout for mobile screens
- [ ] Add touch-friendly controls
- [ ] Improve chart rendering on mobile
- [ ] Add PWA support (optional)

---

## Phase 8: Production Readiness

Prepare for production deployment with monitoring, logging, and documentation.

### Deliverables

- Comprehensive API documentation
- Deployment guide
- Monitoring and observability setup
- Performance benchmarks
- User documentation

### Tasks

**API Documentation**:

- [ ] Generate Swagger/OpenAPI spec from Servant API
- [ ] Add API documentation site (Swagger UI or similar)
- [ ] Write API usage examples
- [ ] Document rate limits and quotas

**Deployment**:

- [ ] Create Docker image for backend
- [ ] Create Docker image for frontend (or static build)
- [ ] Write deployment guide for various platforms
- [ ] Setup CI/CD pipeline for automated deployment
- [ ] Add health checks and readiness probes

**Monitoring**:

- [ ] Add structured logging (JSON format)
- [ ] Integrate with log aggregation service
- [ ] Add metrics collection (Prometheus)
- [ ] Create Grafana dashboards
- [ ] Setup error tracking (Sentry or similar)

**Performance**:

- [ ] Benchmark API endpoints
- [ ] Profile backend performance
- [ ] Optimize frontend bundle size
- [ ] Add performance monitoring (Core Web Vitals)
- [ ] Document performance characteristics

**User Documentation**:

- [ ] Write user guide for calculator
- [ ] Add tooltips and help text throughout UI
- [ ] Create video tutorials (optional)
- [ ] Write glossary of financial terms
- [ ] Add FAQ section

**Security Audit**:

- [ ] Review input validation
- [ ] Test rate limiting
- [ ] Check CORS configuration
- [ ] Review dependency vulnerabilities
- [ ] Add security headers

---

## Future Considerations

### Advanced Analytics

- Implied volatility surface visualization
- Volatility smile/skew analysis
- Option chain data display
- Historical IV percentile tracking

### Machine Learning

- Volatility forecasting with ML models
- Price prediction models
- Anomaly detection in option prices
- Automated strategy recommendation

### Social Features

- User accounts and saved portfolios
- Shared strategy links
- Community strategy library
- Discussion forums

### Mobile Apps

- Native iOS app
- Native Android app
- Offline mode with cached data
- Push notifications for alerts

### Additional Asset Classes

- Traditional equity options
- Forex options
- Commodity options
- Crypto futures and perpetuals
