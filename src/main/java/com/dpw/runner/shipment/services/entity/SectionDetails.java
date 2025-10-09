package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.JoinTable;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.Table;
import java.util.HashSet;
import java.util.Set;

@Entity
@Table(name = "section_details")
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SectionDetails extends MultiTenancy {

    @Column(nullable = false)
    private String sectionName;

    @Column(nullable = false)
    private String sectionDescription;

    @ManyToMany
    @JoinTable(
            name = "section_details_fields",
            joinColumns = @JoinColumn(name = "section_details_id"),
            inverseJoinColumns = @JoinColumn(name = "section_fields_id")
    )
    private Set<SectionFields> sectionFields = new HashSet<>();

}
