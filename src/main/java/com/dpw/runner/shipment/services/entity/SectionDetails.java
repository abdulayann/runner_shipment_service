package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.Table;
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
