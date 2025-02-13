package com.dpw.runner.shipment.services.dto.response.billing;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.*;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class BillBaseResponse implements IRunnerResponse {

    private String billId;
    private String guId;
    private String remarks;

}
