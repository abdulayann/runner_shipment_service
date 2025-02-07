package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.fasterxml.jackson.annotation.JsonIgnore;
import java.util.HashSet;
import java.util.Set;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToMany;
import javax.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

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

  @ManyToMany(mappedBy = "sectionFields")
  @JsonIgnore
  private Set<SectionDetails> sectionDetails = new HashSet<>();


}
