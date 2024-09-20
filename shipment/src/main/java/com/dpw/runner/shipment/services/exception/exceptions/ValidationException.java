package com.dpw.runner.shipment.services.exception.exceptions;

import com.dpw.runner.shipment.services.utils.Generated;

/**
 * Validation Exception: to be thrown whenever validation failed
 */
@Generated
public class ValidationException extends RuntimeException {

    public ValidationException(String msg, Throwable cause) {
        super(msg, cause);
    }

    public ValidationException(String msg) {
        super(msg);
    }
}
