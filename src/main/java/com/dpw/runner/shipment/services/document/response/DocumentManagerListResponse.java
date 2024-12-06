package com.dpw.runner.shipment.services.document.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;
import java.util.List;


@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
public class DocumentManagerListResponse<T> {

    private Boolean success;

    private String requestId;

    private List<T> data;

    private Integer pageNo;

    private Integer pageSize;

    private Long count;

    private String timeStamp;

    private String error;

    private String errorMessage;

    private String successMessage;
}



