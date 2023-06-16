package com.dpw.runner.shipment.services.exception.exceptions;

/**
 * Use this exception when authentication fail
 */

@SuppressWarnings("serial")
public class InvalidAuthenticationException extends RuntimeException{

    public InvalidAuthenticationException(String message) {
        super(message);
    }

    public InvalidAuthenticationException(String message, Throwable cause) {
        super(message, cause);
    }
}

