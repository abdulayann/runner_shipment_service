package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import java.time.LocalDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;


@NoArgsConstructor
@AllArgsConstructor
@Builder
@Getter
@Setter
public class OceanDGRequestLog extends BaseEntity {

  private LocalDateTime time;
  private String userName;
}
