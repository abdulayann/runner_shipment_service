package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import java.util.UUID;

@Data
@Schema(description = "External Document Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class DocumentStoreDto extends CommonRequest implements IRunnerRequest {
    private String fileName;
    private String entityType;
    private UUID entityId;
    private String source;
    private Integer documentMasterId;
    private String encodedFile;
}
