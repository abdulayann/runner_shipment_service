package com.dpw.runner.booking.services.dto.request.npm;

import com.dpw.runner.booking.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class LoadDetailsRequest implements IRunnerRequest {
    private String load_type;
    private String cargo_type;
    private String product_category_code;
    private HazardousInfoRequest hazardous_info;
}
