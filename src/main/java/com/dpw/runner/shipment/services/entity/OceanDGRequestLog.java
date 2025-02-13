package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.*;

import java.time.LocalDateTime;


@NoArgsConstructor
@AllArgsConstructor
@Builder
@Getter
@Setter
public class OceanDGRequestLog extends BaseEntity {

    private LocalDateTime time;
    private String userName;
}
