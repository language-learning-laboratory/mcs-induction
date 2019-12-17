module Distributions
export categorical_sample, Distribution, CatDist, DirCat, DiffCatDist, SimpleCond, support, logscore, sample, add_obs!, rm_obs!

using LogProbs
using StatsFuns.RFunctions: gammarand
using SpecialFunctions: logbeta
import Base: +, -, *, /, zero, one, <, ==

using Flux: Tracker, param

#####################
### Distributions ###
#####################

function categorical_sample(tokens, weights)
    T = eltype(weights)
    x = rand(T) * sum(weights)
    cum_weights = zero(T)
    for (t, w) in zip(tokens, weights)
        cum_weights += w
        if cum_weights > x
            return t
        end
    end
end

categorical_sample(d::Dict) = categorical_sample(keys(d), values(d))
categorical_sample(v::Vector) = categorical_sample(1:length(v), v)

abstract type Distribution{T} end

################################
### Categorical Distribution ###
################################

mutable struct CatDist{T} <: Distribution{T}
    probs :: Dict{T, LogProb}
end

support(cd::CatDist) = keys(cd.probs)

logscore(cd::CatDist, x) = cd.probs[x]
sample(cd::CatDist) = categorical_sample(cd.probs)
add_obs!(cd::CatDist, obs, context) = nothing
rm_obs!(cd::CatDist, obs, context) = nothing

#############################
### Dirichlet Categorical ###
#############################

mutable struct DirCat{T, C} <: Distribution{T}
    counts :: Dict{T, C}
end

DirCat(support, priors) = DirCat(Dict(x => p for (x,p) in zip(support, priors)))
support(dc::DirCat) = keys(dc.counts)

function sample(dc::DirCat)
    weights = [gammarand(c, 1) for c in values(dc.counts)]
    categorical_sample(keys(dc.counts), weights)
end

function logscore(dc::DirCat, obs)
    LogProb(logbeta(sum(values(dc.counts)), 1) - logbeta(dc.counts[obs], 1); islog=true)
end

function add_obs!(dc::DirCat, obs)
    dc.counts[obs] += 1
end

function rm_obs!(dc::DirCat, obs)
    dc.counts[obs] -= 1
end

###############################################
### Differentiable Categorical Distribution ###
###############################################

mutable struct DiffCatDist{T} <: Distribution{T}
    probs :: Dict{T, Tracker.TrackedReal{Float64}}
end

DiffCatDist(support, probs) = DiffCatDist(Dict(r => param(p) for (r,p) in zip(support, probs)))

DiffCatDist(pairs) = DiffCatDist(Dict(r => param(p) for (r,p) in pairs))

support(cd::DiffCatDist) = keys(cd.probs)

function logscore(cd::DiffCatDist, x)
    LogProb(logbeta(sum(values(dc.counts)), 1) - logbeta(dc.counts[obs], 1); islog=true)
end

sample(cd::DiffCatDist) = categorical_sample(cd.probs)
add_obs!(cd::DiffCatDist, obs, context) = nothing
rm_obs!(cd::DiffCatDist, obs, context) = nothing

################################
### Conditional Distribution ###
################################

struct SimpleCond{C, D, S} # context, distribution, support
    dists   :: Dict{C, D}
    support :: S
    SimpleCond(dists::Dict{C, D}, support::S) where {C, D, S} =
        new{C, D, S}(dists, unique(support))
end

function SimpleCond(dists::AbstractDict)
    SimpleCond(
        dists,
        vcat([collect(support(dist)) for dist in values(dists)]...)
    )
end

sample(sc::SimpleCond, context, args...) = sample(sc.dists[context], args...)
logscore(sc::SimpleCond, obs, context) = logscore(sc.dists[context], obs)
rm_obs!(sc::SimpleCond, obs, context) = rm_obs!(sc.dists[context], obs)

score_type(::SimpleCond) = LogProb

function add_obs!(cond::SimpleCond{C,D,S}, obs, context) where {C,D,S}
    if !haskey(cond.dists, context)
        cond.dists[context] = D(cond.support)
    end
    add_obs!(cond.dists[context], obs)
end

end
