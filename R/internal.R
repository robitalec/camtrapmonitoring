###
# reused data.table::setattr wrapper
# set_eval_attr <- function(x, layer, type, direction) {
# 	# buffer size?
# 	# how to flex params + names added
# 	data.table::setattr(x,
# 											'camtrapmonitoring',
# 											list(
# 												layer = layer,
# 												type = type,
# 												direction = direction
# 											))
# }

###
check_type <- function(type) {
	types <- c('categorical', 'binary', 'ordinal', 'real')
	if (!is.null(type)) {
		if (!(type %in% types)) {
			stop('type must be one of ', paste(types, collapse = ', '))
		}
	}
}

check_direction <- function(direction) {
	directions <- c('positive', 'neutral', 'negative')
	if (!is.null(direction)) {
		if (!(direction %in% directions)) {
			stop('direction must be one of ', paste(direction, collapse = ', '))
		}
	}
}