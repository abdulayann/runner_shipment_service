package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import java.util.HashSet;
import java.util.Set;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.JoinTable;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;

@Entity
@Table(name = "section_visibility", uniqueConstraints = @UniqueConstraint(columnNames = {
    "branch_code", "mode", "direction"}))
@Data
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SectionVisibility extends MultiTenancy {

  @Column(name = "branch_code",nullable = false)
  private String branchCode;
  @Column(nullable = false)
  private String mode;
  @Column(nullable = false)
  private String direction;
  @Column(nullable = false)
  private String ticketNumber;
  @ManyToMany
  @JoinTable(
      name = "section_visibility_details",
      joinColumns = @JoinColumn(name = "visibility_id"),
      inverseJoinColumns = @JoinColumn(name = "section_id")
  )
  private Set<SectionDetails> sections = new HashSet<>();
}
