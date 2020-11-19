import numpy as np
import math
import asserts
from abc import ABC, abstractmethod


class CMA_ES(ABC):
    """
    CMA-ES base class with uniform initial population.
    For further details check:
    https://arxiv.org/abs/1604.00772?context=cs
    Implementation based on {cmaes} R package. 
    """

    def __init__(self, x0, lower, upper, **kwargs):
        """
        Function initializes CMA-ES parameters.
        :param: x0 initial point
        :param: lower lower boundry of hyperrectangle
        :param: upper upper boundry of hyperrectangle
        """

        self.init_point = x0
        self.N = x0.size
        asserts.assert_true(
            np.isscalar(lower),
            msg_fmt="lower parameter should be scalar. Given: {got}"
        )
        asserts.assert_true(
            np.isscalar(lower),
            msg_fmt="lower parameter should be scalar. Given: {got}"
        )
        self.lower = lower
        self.upper = upper
        self.fnscale = kwargs.get("fnscale", 1)
        self.stopfitness = kwargs.get("stopfitness", -math.inf)
        self.budget = kwargs.get("budget", 10000 * self.N)
        self.sigma_init = kwargs.get("sigma_init", 1)
        self.sc_tolx = kwargs.get(
            "sc_tolx",
            10 ** (-12) * self.sigma_init
        )
        self.lambda_ = kwargs.get("lambda", 4 * self.N)
        self.maxiter = kwargs.get(
            "maxit",
            math.floor(self.budget / self.lambda_)
        )
        self.mu = kwargs.get("mu", math.floor(self.lambda_ / 2))
        self.weights = kwargs.get(
            "weights",
            math.log(self.mu + 1) - np.log(np.arange(1, self.mu + 1))
        )
        self.weights = self.weights / self.weights.sum()
        self.mueff = kwargs.get(
            "mueff", sum(self.weights) ** 2 / sum(self.weights ** 2)
        )
        self.cc = kwargs.get("cc", 4 / (self.N + 4))
        self.cs = kwargs.get(
            "cs",
            (self.mueff + 2) / (self.N + self.mueff + 3)
        )
        self.mucov = kwargs.get("mucov", self.mueff)
        self.ccov = kwargs.get(
            "ccov",
            (1 / self.mucov) * (2 / ((self.N + 1.4) ** 2))
            + (1 - 1 / self.mucov)
            * ((2 * self.mucov - 1) / ((self.N + 2) ** 2 + 2 * self.mucov)),
        )
        self.damps = kwargs.get(
            "damps",
            1 + 2 * max(0, math.sqrt((self.mueff - 1) / (self.N + 1)) - 1) + self.cs,
        )
        self.chiN = math.sqrt(self.N) * (
            1 - 1 / (4 * self.N) + 1 / (21 * self.N ** 2)
        )
        self.population = np.random.uniform(
            self.lower,
            self.upper,
            (self.N, self.N)
        )
        self.full_display = kwargs.get("self_display", True)

    def adapt_matrix(self, C, BDz, pc, hsig):
        """
        Covariance matrix adaptation with rank-mu and rank-one
        approximation.
        :param: C current covariance matrix
        :param: BDz TODO
        :param: pc current cumulative path for covariance matrix
        :param: hsig safety trick binary flag
        :return: updated covariance matrix
        """

        rank_one = np.outer(pc, pc) + (1 - hsig) * self.cc * (2 - self.cc) * C
        rank_mu = np.matmul(
            BDz,
            np.matmul(np.diag(self.weights), BDz.transpose())
        )
        return (
            (1 - self.ccov) * C
            + self.ccov * (1 / self.mucov) * rank_one
            + self.ccov * (1 - 1 / self.mucov) * rank_mu
        )

    @abstractmethod
    def fmin(self, fn):
        """
        Main loop of CMA-ES algorithm.
        :param fn: evaluation function
        :type fn: [float] -> float 
        """
        pass
