package com.dpw.runner.shipment.services.exception.response;

import com.dpw.runner.shipment.services.utils.Generated;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.springframework.http.HttpStatus;

@SuppressWarnings("unused")
@ApiModel(description = "Api Error Model")
@Generated
public class ApiError {

    @ApiModelProperty(position = 1, name = "status")
    private HttpStatus status;

    @ApiModelProperty(position = 2, name = "message")
    private String message;

    public ApiError(HttpStatus status, String message) {
        this.status = status;
        this.message = message;
    }

    public HttpStatus getStatus() {
        return status;
    }

    public void setStatus(HttpStatus status) {
        this.status = status;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }
}
