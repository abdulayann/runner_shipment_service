package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
@SuppressWarnings("java:S1948")
public class UploadDocumentRequest implements IRunnerRequest {
    private List<MultipartFile> files;
    private Long entityId;
    private String entityType;
    private String docType;
    private Boolean clientEnabled;
    private String eventCode;
    private ByteArrayResource fileResource;
}
