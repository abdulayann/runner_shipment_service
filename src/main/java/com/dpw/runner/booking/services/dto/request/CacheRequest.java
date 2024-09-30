package com.dpw.runner.booking.services.dto.request;

import com.dpw.runner.booking.services.commons.requests.CommonRequest;
import com.dpw.runner.booking.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

@Getter
@Setter
@ApiModel("Cache Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CacheRequest extends CommonRequest implements IRunnerRequest {
    private String key;
}
