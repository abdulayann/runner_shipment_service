package com.dpw.runner.booking.services.dto.response;

import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import lombok.Data;

import java.util.UUID;

@Data
public class FileRepoResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private String fileName;
    private String path;
    private String docType;
    private Boolean clientEnabled;
    private Boolean isPosted;
    private String eventCode;
    private Long entityId;
    private String entityType;
}
