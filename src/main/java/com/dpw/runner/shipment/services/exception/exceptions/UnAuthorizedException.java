package com.dpw.runner.shipment.services.exception.exceptions;

public class UnAuthorizedException extends RuntimeException {
    public UnAuthorizedException(String msg) {
        super(msg);
    }
}
