# implementation of behav object model
# http://adv-r.had.co.nz/S3.html

# TODO: conform to OO conventions? S3?
# how to write to facilitate generic functions?
# set class of object?
ep_subject_task.Behav = function(
						 task=NA,
						 phases=c("default"),	# needs to be a character VECTOR so that output list is named
						 date=NA,
						 time=NA,
						 subject_id=NA,
						 task_number=NA,
						 session_number=NA
						 )
{

	# takes advantage of fact that elements are automatically named by argument
	data = sapply(phases,
					function(phase) {
						return(NA)
					},
					simplify=FALSE)

	qas = sapply(phases,
				 	function(phase) {
					 	x = list(checks=NA, regression_plot=NA, timing_errors=NA)
						#set class here?
						return(x)
					},
					simplify=FALSE)

	meta = list(session_number=session_number, date=date, time=time, subject_id=subject_id, task_number=task_number, task=task)

	return(list(data=data, qa=qas, metadata=meta))
}

ep_subject_task.Eye = function(...)
{
	return(ep_subject_task.Behav(...))
}

ep_subject_task.Physio = function(...)
{
	return(ep_subject_task.Behav(...))
}
