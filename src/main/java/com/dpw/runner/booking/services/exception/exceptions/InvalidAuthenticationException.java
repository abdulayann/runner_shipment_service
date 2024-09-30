package com.dpw.runner.booking.services.exception.exceptions;

import com.dpw.runner.booking.services.utils.Generated;

/**
 * Use this exception when authentication fail
 */

@SuppressWarnings("serial")
@Generated
public class InvalidAuthenticationException extends RuntimeException{

    public InvalidAuthenticationException(String message) {
        super(message);
    }

    public InvalidAuthenticationException(String message, Throwable cause) {
        super(message, cause);
    }
}

