package com.dpw.runner.shipment.services.document.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;


@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
public class DocumentManagerResponse<T> {

    private Boolean success;

    private String requestId;

    private T data;

    private Integer pageNo;

    private Integer pageSize;

    private Long count;

    private String timeStamp;

    private String error;

    private String errorMessage;

    private String successMessage;
}



