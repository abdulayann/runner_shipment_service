package com.dpw.runner.shipment.services.document.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;


@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
public class DocumentManagerFileResponse {
    private Long fileId;

    private String guid;

    private Long ruleId;

    private String docCode;

    private String docName;

    @JsonProperty("isMandatory")
    private Boolean isMandatory;

    private String source;

    private String createdAt;

    private String validFrom;

    private String validTo;

    private String physicalDocWith;

    private String fileName;

    private Long fileSize;

    private String fileType;

    private String path;

    private String fileGuid;

    private String entityType;

    private Long entityId;

    private String childType;

    private String childId;

    private String eventCode;

    private String fileSource;
}
