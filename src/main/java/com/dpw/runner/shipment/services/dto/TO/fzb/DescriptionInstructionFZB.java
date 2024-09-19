package com.dpw.runner.shipment.services.dto.TO.fzb;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Getter
@NoArgsConstructor
@Setter
@AllArgsConstructor
public class DescriptionInstructionFZB {

    @NotBlank(message = "Description cannot be blank")
    @Size(max = 70, message = "Description length must be less than or equal to 70 characters")
    private String description;

    @NotBlank(message = "Description code cannot be blank")
    @Size(max = 3, message = "Description code length must be 3 characters")
    @Pattern(regexp = "[A-Za-z]{3}", message = "Description code must contain only alphabets and be 3 characters long")
    private String descriptionCode;
}
