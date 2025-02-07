package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
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
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "section_visibility", uniqueConstraints = @UniqueConstraint(columnNames = {
    "branch", "mode", "direction"}))
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SectionVisibility extends BaseEntity {

  @Column(nullable = false)
  private String branch;
  @Column(nullable = false)
  private String mode;
  @Column(nullable = false)
  private String direction;

  @ManyToMany
  @JoinTable(
      name = "section_visibility_details",
      joinColumns = @JoinColumn(name = "visibility_id"),
      inverseJoinColumns = @JoinColumn(name = "section_id")
  )
  private Set<SectionDetails> sections = new HashSet<>();
}
