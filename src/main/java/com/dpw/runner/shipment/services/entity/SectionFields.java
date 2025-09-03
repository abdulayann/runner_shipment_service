package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name = "section_fields")
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SectionFields extends BaseEntity {

    @Column(nullable = false, unique = true)
    private String fieldName;

    @Column(nullable = false)
    private String fieldDescription;

}
