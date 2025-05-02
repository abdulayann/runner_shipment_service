package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.config.CustomVolumeValueSerializer;
import com.dpw.runner.shipment.services.config.CustomWeightValueSerializer;
import com.dpw.runner.shipment.services.config.DecimalPlaceValueSerializer;
import com.dpw.runner.shipment.services.entity.enums.ContainerPraStatus;
import com.dpw.runner.shipment.services.entity.enums.ContainerStatus;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import io.swagger.annotations.ApiModel;
import lombok.Data;
import lombok.Setter;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Data
@Setter
@ApiModel("Container Response Model")
public class ContainerListResponse implements IRunnerResponse {
    private List<ContainerBaseResponse> containers;
    @JsonIgnore
    private Integer totalPages;
    @JsonIgnore
    private Long totalCount;
}
