package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

@Data
@ApiModel("External Document Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ExternalDocumentRequest extends CommonRequest implements IRunnerRequest {
    private String fileName;
    private String fileType;
    private Integer documentMasterId;
    private String docContent;
}
