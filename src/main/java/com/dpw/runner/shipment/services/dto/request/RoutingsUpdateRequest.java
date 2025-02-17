package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import lombok.*;

import javax.validation.constraints.NotEmpty;
import java.util.List;

@Getter
@Setter
@Builder
@ApiModel("Routings Bulk Update Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class RoutingsUpdateRequest implements IRunnerRequest {

    @NotEmpty(message = "routings cannot be blank / null")
    @JsonProperty("routings")
    private List<RoutingsRequest> routingsRequests;
}
