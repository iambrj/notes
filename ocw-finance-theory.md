---
fontfamily: mathpazo
title: 'Notes on MIT OCW Course "Finance Theory"'
author: 'Bharathi Ramana Joshi'
date: 'dd-mm-2021'
header-includes:
  - \hypersetup{colorlinks=true, urlcolor=blue}
---

- Chapters notes are from "Principles of Corporate Finance", Brealey, Myers, and Allen

# Lecture 1
- Finance theory = valuation + management (acquiring/selling)

## Accounting 101
- Stock: level of water in the bathtub, flow: rate of water coming from tap
- Balance sheet: snapshot of financial status quo (stock)
- Income statement: rate of change of balance sheet (flow)

## Fundamental Principles of Finance
1. No such thing as a free lunch (program)
2. Other things equal, individuals:
  * Prefer more money to less (non-satiation)
  * Prefer money now to later (impatience)
  * Prefer to avoid risk (risk aversino)
3. All agents act to further their own self-interest
4. Financial market prices shift to equalize supply and demand
5. Financial markets are highly adaptive and competitive
6. Risk-sharing and frictions are central to financial innovation

# Lecture 2
- What is an asset?
  + Business entity
  + Property, plant, equipment
  + Patents, R&D
  + Stocks, bonds, options
  + Knowledge, reputation, 
- Patent: process to create assets from knowledge. Knowledge is public, but to
  employ that knowledge permission must be obtained from patent owner.
- Business/trade secret: knowledge is secret, but if secret leaks no permission
  is required to employ it.
- Cashflow: money that comes or goes
- Asset is a sequence of future cashflows at any given time
  `Asset_t = {CF_t, CF_(t + 1), ...}`
- How to value a asset? Use value operator `V_t`
  Value of asset = `V_t(CF_t, CF_(t + 1), ...)`
- Cashflows at different times are in different currencies! Appropriate exchange
  rates must be used when working with them
- Net Present Value (NPV)
  `V_0(CF_1, CF_2, ...) = CF_0 + ($1/$0) * CF_1 + ($2/$0) * CF_2 + ...`
- Exchange rate `r` (gotten from market):
  Value of today `$1 = $1 * (1 + r)^t` after t years

# Chapter 1
- Assets are either tangible (e.g. land, machinery) or intangible (e.g. brand
  name, patent)
- Two questions financial manager needs to answer:
  1. What investments to make?
  2. How to fund those investments?
- Broad themes
  1. Corporate finance is all about maximising value
  2. The opportunity costs of capital sets the standard for investment decisions
  3. A safe dolar is worth more than a risky dollar
  4. Smart investment decisions create more value than smart financing decisions
  5. Good governance matters
- Real assets: they have inherent physical value (e.g. land, infrastructure)
- Financial assets/securities: their value is derived from contractual claim
  (e.g. bonds, participation in share capital, bank deposits)
- Capital budgeting/expenditure: investment decisions
- Capital structure decision: debt or equity financing (e.g. shareholders get a
    fraction of future profits and cash flow)
- Payout decision: pay dividends (distribute profits) or repurchase stock
- Corporation: legal entity. Pays taxes but cannot vote :(
- Limited liability: shareholders are not personally responsible for
  corporation's debts. Shareholders can lose their entire investment in the
  corporation but no more. Sole proprietorships and partners face unlimited
  liability
- Key characteristic distinguishing corporation from
  proprietorships/partnerships: separation of ownership and management
- Goal: maximize shareholder value
