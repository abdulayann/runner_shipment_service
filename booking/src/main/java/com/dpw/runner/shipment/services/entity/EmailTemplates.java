package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Getter
@Setter
@Table(name = "email_templates")
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE email_templates SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class EmailTemplates extends MultiTenancy {

    @Column(name = "name")
    private String name;

    @Column(name = "type")
    private String type;

    @Column(name = "body")
    private String body;

    @Column(name = "subject")
    private String subject;

    @Column(name = "shipment_settings_id")
    private Long shipmentSettingsId;
}
