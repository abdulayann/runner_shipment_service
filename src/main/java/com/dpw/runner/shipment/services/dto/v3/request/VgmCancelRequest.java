package com.dpw.runner.shipment.services.dto.v3.request;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import jakarta.validation.constraints.NotNull;

import jakarta.validation.constraints.Min;
import java.io.Serializable;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class VgmCancelRequest implements Serializable {
    @NotNull(message = "Containers id can not be null/empty")
    @Min(value = 1, message = "Min containers id should be one")
    private List<Long> containersIds;
}
