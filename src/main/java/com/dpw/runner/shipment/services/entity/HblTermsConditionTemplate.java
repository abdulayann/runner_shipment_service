package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.TypeOfHblPrint;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import javax.validation.constraints.Size;

@Entity
@Getter
@Setter
@Table(name = "hbl_terms_condition_template")
@NoArgsConstructor
@AllArgsConstructor
@Where(clause = "is_deleted = false")
public class HblTermsConditionTemplate extends MultiTenancy {

    @Column(name = "template_code")
    @Size(max=50, message = "max size is 50 for template_code")
    private String templateCode;

    @Column(name = "template_file_name")
    @Size(max=100, message = "max size is 100 for template_file_name")
    private String templateFileName;

    @Column(name = "shipment_settings_id")
    private Long shipmentSettingsId;

    @Column(name = "is_front_print")
    private Boolean isFrontPrint;

    @Enumerated(EnumType.ORDINAL)
    @Column(name = "type_of_hbl_print")
    private TypeOfHblPrint typeOfHblPrint;

    @Column(name = "is_watermark_required")
    private Boolean isWaterMarkRequired;
}
