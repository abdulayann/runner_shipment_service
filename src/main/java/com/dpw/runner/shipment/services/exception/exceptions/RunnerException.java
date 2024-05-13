package com.dpw.runner.shipment.services.exception.exceptions;

/**
 * Use this exception in general exception case.
 */

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
