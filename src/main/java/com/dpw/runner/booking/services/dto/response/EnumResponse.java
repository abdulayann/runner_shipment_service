package com.dpw.runner.booking.services.dto.response;

import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.List;
import java.util.Map;


@Data
@Builder
@ApiModel("Enum Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class EnumResponse implements IRunnerResponse {
    private Map<String, List<EnumConstantResponse>> dataMap;
}
