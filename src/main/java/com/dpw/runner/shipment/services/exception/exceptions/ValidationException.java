package com.dpw.runner.shipment.services.exception.exceptions;

/**
 * Validation Exception: to be thrown whenever validation failed
 */

public class ValidationException extends RuntimeException {

    public ValidationException(String msg, Throwable cause) {
        super(msg, cause);
    }

    public ValidationException(String msg) {
        super(msg);
    }
}
