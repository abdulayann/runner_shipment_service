package com.dpw.runner.shipment.services.document.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;

import java.util.List;

@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
@SuppressWarnings("java:S1948")
public class DocumentManagerDataResponse implements IRunnerResponse {
    private String path;

    private String fileName;

    private String secureDownloadLink;

    private String successMsg;

    private String guid;

    private Long fileSize;

    private String fileType;

    private String outputBase64;

    private String fileGuid;

    private String fileId;

    private String timeUploaded;

    private List<DocumentManagerFileResponse> files;
}
