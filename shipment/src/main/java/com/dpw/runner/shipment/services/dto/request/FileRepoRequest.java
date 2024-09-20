package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

@Getter
@Setter
@ApiModel("File Repository Request Model")
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class FileRepoRequest extends CommonRequest implements IRunnerRequest {
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
