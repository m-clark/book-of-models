ols = function(x, y, b0, b1, sum = FALSE) {
    # Calculate the predicted values
    y_hat = b0 + b1 * x

    # Calculate the error
    error = y - y_hat

    # Calculate the value as sum or mean squared error
    if (sum) {
        value = sum(error^2, na.rm = TRUE)
    } else {
        value = mean(error^2, na.rm = TRUE)
    }

    # Return the value
    return(value)
}