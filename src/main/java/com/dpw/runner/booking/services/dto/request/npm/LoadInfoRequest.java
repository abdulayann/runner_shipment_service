package com.dpw.runner.booking.services.dto.request.npm;

import com.dpw.runner.booking.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class LoadInfoRequest implements IRunnerRequest {
    private String operation;
    private LoadDetailsRequest load_details;
    private LoadAttributesRequest load_attributes;
}
