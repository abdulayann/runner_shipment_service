package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

@Data
@Builder
@Schema("MAWB Stocks Link Request Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class MawbStocksLinkRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private String mawbNumber;
    private String status;
    private String seqNumber;
    private String entityType;
    private Integer entityId;
    private String shipConsNumber;
}
