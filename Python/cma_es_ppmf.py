from cma_es import CMA_ES
import numpy as np
import pandas as pd
import math
import SigmaPolicy
from utils import cast_on_boundries, is_pos_def


class CMA_ES_PPMF(CMA_ES):
    """
    CMA-ES with PPMF (Previous Population Midpoint Fitness) rule.
    """

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.sigma_policy = SigmaPolicy.ppmf
        self.lambda_ = kwargs.get(
            "lambda",
            4 * self.N - 1
        )
        self.p_target = kwargs.get(
            "p_target",
            0.1
        )
        self.d_param = kwargs.get(
            "d_param",
            2
        )

    def fmin(self, fn):
        """
        :param fn evalutation function
        """
        xmean = self.init_point
        pc = np.zeros((1, self.N))
        ps = np.zeros((1, self.N))
        B = np.identity(self.N)
        D = np.identity(self.N)
        BD = np.matmul(B, D)
        C = np.matmul(BD, BD.transpose())
        sigma = self.sigma_init

        eval_mean = math.inf
        eval_meanOld = math.inf

        self.best_fit = math.inf
        self.best_par = None
        self.iter = 0
        self.counteval = 0
        self.cviol = 0
        self.msg = None

        log_scalars = pd.DataFrame(columns=["iter", "sigma", "best_fit", "mean_fit"])

        while self.counteval < self.budget:
            iter_log = {}
            self.iter += 1

            iter_log["iter"] = self.iter

            arz = np.random.normal(0, 1, (self.N, self.lambda_))
            arx = xmean[:, None] + sigma * np.matmul(BD, arz)
            vx = cast_on_boundries(arx, self.lower, self.upper)

            penalty = 1 + np.apply_along_axis(sum, 0, (arx - vx) ** 2)
            self.cviol += np.where(penalty > 1)[0].sum()
            y = np.apply_along_axis(fn, 0, vx)
            self.counteval += self.lambda_
            arfitness = y * penalty

            valid_points_indices = np.where(penalty <= 1)[0]
            if any(valid_points_indices):
                best_ind = y[valid_points_indices].argmin()
                if y[valid_points_indices][best_ind] < self.best_fit:
                    self.best_fit = y[valid_points_indices][best_ind]
                    self.best_par = arx[:, valid_points_indices][:, best_ind]

            arindex = arfitness.argsort()
            arfitness = arfitness[arindex]
            aripop = arindex[0 : (self.mu)]
            iter_log["best_fit"] = arfitness[0]
            selx = arx[:, aripop]
            xmean = np.matmul(selx, self.weights)
            selz = arz[:, aripop]
            zmean = np.matmul(selz, self.weights)

            ps = (1 - self.cs) * ps + math.sqrt(
                self.cs * (2 - self.cs) * self.mueff
            ) * np.matmul(B, zmean)
            hsig_lhs = (
                np.linalg.norm(ps)
                / math.sqrt(1 - (1 - self.cs) ** (2 * (self.counteval / self.lambda_)))
                / self.chiN
            )
            hsig_rhs = 1.4 + 2 / (self.N + 1)
            hsig = hsig_lhs < hsig_rhs
            pc = (1 - self.cc) * pc + hsig * math.sqrt(
                self.cc * (2 - self.cc) * self.mueff
            ) * np.matmul(BD, zmean)


            eval_meanOld = eval_mean
            mean_point = np.apply_along_axis(np.mean, 0, vx)
            eval_mean = fn(mean_point) 
            self.counteval += 1

            BDz = np.matmul(BD, selz)

            C = self.adapt_matrix(C, BDz, pc, hsig)

            iter_log["sigma"] = sigma

            sigma = self.sigma_policy(
                sigma,
                eval_meanOld,
                arfitness,
                self.d_param,
                self.p_target
            )

            log_iter["mean_fit"] = eval_mean

            eigen_values, eigen_vectors = np.linalg.eigh(C)

            if not is_pos_def(eigen_values):
                self.msg = "Covariance matrix is numerically not positive definite."
                break

            B = eigen_vectors

            D = np.diag(np.sqrt(eigen_values))

            BD = np.matmul(B, D)

            iter_log["sigma"] = arfitness[0]

            log_scalars = log_scalars.append(iter_log, ignore_index = True)
            if arfitness[0] <= self.stopfitness * self.fnscale:
                self.msg = "Stop fitness reached"
                break

            if all((D < self.sc_tolx).flatten()) and all(
                (sigma * pc < self.sc_tolx).flatten()
            ):
                self.msg = "All standard deviations smaller than tolerance."
                break

            if (
                arfitness[0]
                == arfitness[
                    min(
                        1 + math.floor(self.lambda_ / 2),
                        2 + math.ceil(self.lambda_ / 4),
                    )
                ]
            ):
                sigma = sigma * math.exp(0.2 + self.cs / self.damps)

            if self.full_display:
                return (self.best_fit, log_scalars)
            else:
                return self.best_fit
