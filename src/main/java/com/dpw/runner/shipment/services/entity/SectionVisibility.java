package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import java.util.HashSet;
import java.util.Set;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
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
