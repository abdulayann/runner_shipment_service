package com.dpw.runner.shipment.services.dto.request.npm;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UpdateContractRequest implements IRunnerRequest {
    private String contract_id;
    private String contract_state;
    private String source;
    private String source_type;
    private Boolean is_alteration;
    private BusinessInfo business_info;
    private List<LoadInfoRequest> loads_info;


    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class BusinessInfo implements Serializable {
        private String product_name;
        private String tenant_id;
    }
}
