package com.dpw.runner.booking.services.dto.response;

import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import lombok.*;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
public class TemplateUploadResponse implements IRunnerResponse {
    private String templateId;
    private String templateName;
}
