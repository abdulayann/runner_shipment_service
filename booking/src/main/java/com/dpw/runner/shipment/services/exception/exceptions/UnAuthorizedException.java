package com.dpw.runner.shipment.services.exception.exceptions;

import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.UNAUTHORIZED)
@Generated
public class UnAuthorizedException extends RuntimeException {
    public UnAuthorizedException(String msg) {
        super(msg);
    }
}
