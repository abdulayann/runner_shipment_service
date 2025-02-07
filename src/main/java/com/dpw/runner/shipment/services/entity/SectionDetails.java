package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.fasterxml.jackson.annotation.JsonIgnore;
import java.util.HashSet;
import java.util.Set;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "section_details")
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SectionDetails extends BaseEntity {

  @Column(nullable = false, unique = true)
  private String sectionName;

  @Column(nullable = false)
  private String sectionDescription;

  @ManyToMany
  @JoinTable(
      name = "section_details_fields",
      joinColumns = @JoinColumn(name = "section_details_id"),
      inverseJoinColumns = @JoinColumn(name = "section_fields_id")
  )
  @JsonIgnore
  private Set<SectionFields> sectionFields = new HashSet<>();

  @ManyToMany(mappedBy = "sections")
  @JsonIgnore
  private Set<SectionVisibility> sectionVisibilities = new HashSet<>();
}
