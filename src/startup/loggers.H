#ifndef LOGGERS_H
#define LOGGERS_H

#include <vector>
#include <string>
#include <models/probability-model.H>
#include "owned-ptr.H"
#include "mcmc/logger.H"

std::vector<owned_ptr<MCMC::Logger> > construct_loggers(owned_ptr<Probability_Model>& M, const std::vector<std::string>& Rao_Blackwellize, int proc_id, const std::string& dir_name);

#endif
