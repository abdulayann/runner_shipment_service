package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@Table(name = "app_config")
@Entity
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AppConfig extends BaseEntity {

  @Column(name = "key", length = 50, unique = true, nullable = false)
  private String key;

  @Column(name = "value", length = 350, nullable = false)
  private String value;

}

