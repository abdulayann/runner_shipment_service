package com.dpw.runner.booking.services.dto.request.billing;

import com.dpw.runner.booking.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class LastPostedInvoiceDateRequest implements IRunnerRequest {
    private String moduleType;
    private String moduleGuid;
}
