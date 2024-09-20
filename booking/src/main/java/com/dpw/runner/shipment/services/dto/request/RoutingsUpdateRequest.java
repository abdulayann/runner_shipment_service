package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import java.util.List;
import javax.validation.constraints.NotEmpty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

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
