package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;
import org.springframework.web.multipart.MultipartFile;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
@SuppressWarnings("java:S1948")
public class TemplateUploadRequest implements IRunnerRequest {
    private MultipartFile file;
    private String previousFileId;
}
