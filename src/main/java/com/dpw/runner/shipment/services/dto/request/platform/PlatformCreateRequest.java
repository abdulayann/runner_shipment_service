package com.dpw.runner.shipment.services.dto.request.platform;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;

@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@Data
public class PlatformCreateRequest extends CommonRequest implements IRunnerRequest {
    private String booking_ref_code;
    private String business_code;
    private String origin_code;
    private String destination_code;
    private String pol;
    private String pod;
    private String contract_id;
    private String customer_org_id;
    private LocalDateTime created_at;
    private List<LoadRequest> load;
    private RouteRequest route;
    private List<ChargesRequest> charges;
    private OrgRequest bill_to_party;

}
