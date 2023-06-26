package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

@Data
public class FileRepoResponse implements IRunnerResponse {
    private Long id;
    private String fileName;
    private String path;
    private String docType;
    private Boolean clientEnabled;
    private Boolean isPosted;
    private String eventCode;
    private Long entityId;
    private String entityType;
}
