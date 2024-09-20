package com.dpw.runner.shipment.services.exception.exceptions;

import com.dpw.runner.shipment.services.utils.Generated;

/**
 * Use this exception in general exception case.
 */
@Generated
public class RunnerException extends Exception {

    public RunnerException(String msg, Throwable cause) {
        super(msg, cause);
    }

    public RunnerException(String msg) {
        super(msg);
    }

    public RunnerException() {

    }
}
