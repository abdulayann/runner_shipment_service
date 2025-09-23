package com.dpw.runner.shipment.services.exception.exceptions;

import com.dpw.runner.shipment.services.utils.Generated;
import lombok.Getter;

import java.util.List;

/**
 * This exception is thrown when multiple validation issues are found
 * during request processing. It is typically used to aggregate
 * field-level validation errors and return them together in the response.
 */
@Getter
@Generated
public class MultiValidationException extends RuntimeException {
    private final List<String> errors;

    public MultiValidationException(String msg, Throwable cause, List<String> errors) {
        super(msg, cause);
        this.errors = errors;
    }

    public MultiValidationException(String msg, List<String> errors) {
        super(msg);
        this.errors = errors;
    }

    public MultiValidationException(String msg) {
        super(msg);
        this.errors = null;
    }

}