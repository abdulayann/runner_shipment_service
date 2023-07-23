package com.dpw.runner.shipment.services.dto.request;

import lombok.*;

import java.util.List;


@Data
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class HblCargoDataDto {

    private List<HblCargoDto> cargos;
}