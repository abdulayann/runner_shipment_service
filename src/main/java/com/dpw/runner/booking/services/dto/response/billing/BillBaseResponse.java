package com.dpw.runner.booking.services.dto.response.billing;

import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

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
